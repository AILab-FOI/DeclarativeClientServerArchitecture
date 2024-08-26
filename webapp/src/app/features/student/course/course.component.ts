import {
  Component,
  computed,
  effect,
  inject,
  input,
  Input,
  signal,
} from '@angular/core';
import { dohvati_studenta_na_kolegiju } from '../../../../assets/pkg/client';
import { StudentKolegij, UserService } from '../../../core';
import { StudentParticipantsComponent } from './participants/participants.component';
import { StudentGradesComponent } from './grades/grades.component';
import { ButtonComponent } from '../../../shared/button/button.component';
import { TokenService } from '../../../core/services/token.service';

@Component({
  selector: 'app-course',
  standalone: true,
  imports: [
    ButtonComponent,
    StudentParticipantsComponent,
    StudentGradesComponent,
  ],
  templateUrl: './course.component.html',
  styleUrl: './course.component.scss',
})
export class StudentCourseComponent {
  public user = inject(UserService);
  private token = inject(TokenService);
  public id = input.required<number>();
  public course = computed(() => {
    return this.student_course()?.kolegij;
  });
  public participants = computed(() => {
    return this.course()?.studenti;
  });
  public student_course = signal<StudentKolegij>(undefined);
  public page = signal<number>(0);

  constructor() {
    effect(() => {
      dohvati_studenta_na_kolegiju(
        this.user.user().id,
        this.id(),
        this.token.accessToken(),
      ).then((res) => {
        this.student_course.set(res);
      });
    });
  }

  public setPage(page: number): void {
    this.page.set(page);
  }
}
