import { Component, computed, inject } from '@angular/core';
import { UserService } from '../../core';
import { CourseCardComponent } from '../../shared/course-card/course-card.component';

@Component({
  selector: 'app-courses',
  standalone: true,
  imports: [CourseCardComponent],
  templateUrl: './courses.component.html',
  styleUrl: './courses.component.scss',
})
export class CoursesComponent {
  public user = inject(UserService);
  public courses = computed(() => {
    return this.user.user().kolegiji;
  });
}
