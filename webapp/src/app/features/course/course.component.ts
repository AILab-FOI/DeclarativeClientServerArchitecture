import {
  Component,
  computed,
  effect,
  inject,
  input,
  Input,
  signal,
} from '@angular/core';
import { dohvati_korisnika_na_kolegiju } from '../../../assets/pkg/client';
import { StudentKolegij, UserService } from '../../core';
import { ParticipantsComponent } from './participants/participants.component';
import { GradesComponent } from './grades/grades.component';
import { ButtonComponent } from '../../shared/button/button.component';

@Component({
  selector: 'app-course',
  standalone: true,
  imports: [ButtonComponent, ParticipantsComponent, GradesComponent],
  templateUrl: './course.component.html',
  styleUrl: './course.component.scss',
})
export class CourseComponent {
  public user = inject(UserService);
  public id = input.required<number>();
  public course = computed(() => {
    return this.student_course()?.kolegij;
  });
  public student_course = signal<StudentKolegij>(undefined);
  public page = signal<number>(0);

  constructor() {
    effect(() => {
      dohvati_korisnika_na_kolegiju(
        this.user.user().id,
        this.id(),
        localStorage.getItem('AT'),
      ).then((res) => {
        console.log(res);
        this.student_course.set(res);
      });
    });
  }

  public setPage(page: number): void {
    this.page.set(page);
  }
}
