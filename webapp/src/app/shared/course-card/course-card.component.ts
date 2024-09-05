import { Component, inject, input } from '@angular/core';
import { Kolegij, UserService } from '../../core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-course-card',
  standalone: true,
  imports: [],
  templateUrl: './course-card.component.html',
  styleUrl: './course-card.component.scss',
})
export class CourseCardComponent {
  public course = input.required<Kolegij>();
  public clickable = input<boolean>(true);
  public router = inject(Router);
  public user = inject(UserService);

  public goTo(): void {
    if (this.clickable()) {
      this.router.navigate(['course', this.course().id]);
    }
  }
}
